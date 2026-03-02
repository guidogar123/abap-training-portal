import { useState, useEffect } from 'react';
import { ChevronRight, ChevronDown, BookOpen, Hexagon, Play, LogOut, User as UserIcon } from 'lucide-react';
import { Week, Session } from '../data/courseContent';
import { useAuth } from '../context/AuthContext';

interface SidebarProps {
    weeks: Week[];
    currentSession: Session;
    viewMode: 'course' | 'playground';
    onSelectSession: (session: Session) => void;
    onSelectPlayground: () => void;
    isOpen: boolean;
    toggleSidebar?: () => void;
}

const Sidebar = ({ weeks, currentSession, viewMode, onSelectSession, onSelectPlayground, isOpen }: SidebarProps) => {
    const [expandedWeeks, setExpandedWeeks] = useState<Record<number, boolean>>({});
    const { user, logout } = useAuth();

    const toggleWeek = (weekId: number) => {
        setExpandedWeeks(prev => ({ ...prev, [weekId]: !prev[weekId] }));
    };

    // Expand first week by default
    useEffect(() => {
        if (weeks.length > 0 && Object.keys(expandedWeeks).length === 0) {
            setExpandedWeeks({ [weeks[0].id]: true });
        }
    }, [weeks]);

    return (
        <div className={`
      fixed inset-y-0 left-0 z-20 w-80 bg-[var(--sidebar-bg)] border-r border-[var(--border-color)] flex flex-col transform transition-transform duration-300 ease-in-out
      ${isOpen ? 'translate-x-0' : '-translate-x-full'}
      md:relative md:translate-x-0
    `}>
            {/* Header */}
            <div className="p-6 border-b border-white/10 flex items-center gap-3 shrink-0">
                <div className="w-10 h-10 rounded-xl bg-gradient-to-br from-blue-500 to-purple-600 flex items-center justify-center text-white font-bold shadow-lg shadow-blue-500/20">
                    <Hexagon size={24} fill="currentColor" className="text-white/20" />
                </div>
                <div>
                    <h1 className="text-lg font-bold text-white tracking-tight leading-tight">ABAP Master</h1>
                    <p className="text-[10px] text-blue-400 font-bold uppercase tracking-wider">Sistemas e Información</p>
                </div>
            </div>

            {/* Navigation Content */}
            <div className="flex-1 overflow-y-auto p-4 space-y-4 custom-scrollbar">
                {/* Herramientas / Tools */}
                <div className="space-y-1 mb-6">
                    <p className="px-3 text-[10px] font-bold text-slate-500 uppercase tracking-widest mb-2 flex items-center gap-2">
                        <span className="w-1 h-1 rounded-full bg-blue-400"></span>
                        Herramientas
                    </p>
                    <button
                        onClick={() => onSelectPlayground && onSelectPlayground()}
                        className={`
                            w-full flex items-center gap-3 px-4 py-3 rounded-xl text-sm font-bold transition-all border
                            ${viewMode === 'playground'
                                ? 'bg-blue-600 border-blue-400 text-white shadow-lg shadow-blue-900/50 scale-[1.02]'
                                : 'bg-slate-800/80 border-white/5 text-slate-200 hover:bg-slate-700 hover:text-white shadow-md'}
                        `}
                    >
                        <Play size={16} fill={viewMode === 'playground' ? 'currentColor' : 'none'} className={viewMode === 'playground' ? 'text-white' : 'text-blue-400'} />
                        ABAP Playground
                    </button>
                </div>

                <div className="space-y-2">
                    <p className="px-3 text-[10px] font-bold text-slate-500 uppercase tracking-widest mb-2">Contenido del Curso</p>
                    {weeks.map((week) => (
                        <div key={week.id} className="rounded-xl overflow-hidden bg-slate-800/20 border border-white/5">
                            <button
                                onClick={() => toggleWeek(week.id)}
                                className="w-full flex items-center justify-between p-3 text-left hover:bg-white/5 transition-colors"
                            >
                                <div className="flex items-center gap-3">
                                    <span className="flex items-center justify-center w-6 h-6 rounded-lg bg-slate-800 border border-white/10 text-[10px] font-bold text-blue-400 shadow-inner">
                                        W{week.id}
                                    </span>
                                    <span className="text-sm font-semibold text-slate-200">
                                        {week.title.replace('Semana ' + week.id + ': ', '')}
                                    </span>
                                </div>
                                {expandedWeeks[week.id] ? <ChevronDown size={14} className="text-slate-500" /> : <ChevronRight size={14} className="text-slate-500" />}
                            </button>

                            {expandedWeeks[week.id] && (
                                <div className="bg-black/20 pb-2">
                                    {week.sessions.map((session) => (
                                        <button
                                            key={session.id}
                                            onClick={() => onSelectSession(session)}
                                            className={`
                       w-full flex items-center gap-3 px-4 py-2.5 text-xs transition-all border-l-2
                       ${currentSession?.id === session.id
                                                    ? 'border-blue-500 bg-blue-500/10 text-blue-200 font-bold'
                                                    : 'border-transparent text-slate-400 hover:text-slate-100 hover:bg-white/5'}
                     `}
                                        >
                                            <BookOpen size={14} className={currentSession?.id === session.id ? 'opacity-100 text-blue-400' : 'opacity-40'} />
                                            <span className="truncate">{session.title.split(': ')[1] || session.title}</span>
                                        </button>
                                    ))}
                                </div>
                            )}
                        </div>
                    ))}
                </div>
            </div>

            {/* User Footer */}
            <div className="p-4 border-t border-white/10 bg-slate-900/50 shrink-0">
                <div className="flex items-center gap-3 mb-4 p-2 rounded-xl bg-white/5 border border-white/5">
                    <div className="w-10 h-10 rounded-lg bg-slate-800 flex items-center justify-center border border-white/10 text-slate-400">
                        <UserIcon size={20} />
                    </div>
                    <div className="overflow-hidden">
                        <p className="text-sm font-bold text-white truncate">{user?.name || 'Consultor ABAP'}</p>
                        <p className="text-[10px] text-slate-500 truncate">{user?.username || 'user@sistemaseinformacion.com'}</p>
                    </div>
                </div>

                <button
                    onClick={logout}
                    className="w-full flex items-center justify-center gap-2 px-4 py-2.5 rounded-xl text-xs font-bold text-red-400 hover:text-red-300 hover:bg-red-400/10 border border-transparent hover:border-red-400/20 transition-all"
                >
                    <LogOut size={14} />
                    Cerrar Sesión
                </button>
            </div>
        </div>
    );
};

export default Sidebar;
