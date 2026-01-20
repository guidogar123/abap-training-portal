import { useState } from 'react';
import Sidebar from './components/Sidebar';
import LessonContent from './components/LessonContent';
import { courseData, Session } from './data/courseContent';
import { Menu, X } from 'lucide-react';
import { AbapPlayground } from './components/AbapPlayground';

function App() {
    const [viewMode, setViewMode] = useState<'course' | 'playground'>('course');
    const [currentSession, setCurrentSession] = useState<Session>(courseData.weeks[0].sessions[0]);
    const [sidebarOpen, setSidebarOpen] = useState(false);

    return (
        <div className="flex h-screen overflow-hidden bg-[var(--bg-color)] text-[var(--text-primary)]">
            {/* Mobile Menu Button */}
            <button
                className="md:hidden fixed top-4 right-4 z-50 p-2 bg-slate-800 rounded-lg text-white border border-slate-700 shadow-lg"
                onClick={() => setSidebarOpen(!sidebarOpen)}
            >
                {sidebarOpen ? <X /> : <Menu />}
            </button>

            {/* Sidebar Overlay */}
            {sidebarOpen && (
                <div
                    className="fixed inset-0 bg-black/60 backdrop-blur-sm z-10 md:hidden"
                    onClick={() => setSidebarOpen(false)}
                />
            )}

            <Sidebar
                weeks={courseData.weeks}
                currentSession={currentSession}
                viewMode={viewMode}
                onSelectSession={(session) => {
                    setCurrentSession(session);
                    setViewMode('course');
                    setSidebarOpen(false);
                }}
                onSelectPlayground={() => {
                    setViewMode('playground');
                    setSidebarOpen(false);
                }}
                isOpen={sidebarOpen}
            />

            <main className="flex-1 overflow-y-auto px-4 md:px-8 py-8 md:py-12 scroll-smooth relative">
                <div className="absolute top-0 left-0 w-full h-96 bg-gradient-to-b from-blue-900/10 to-transparent pointer-events-none" />
                <div className="relative z-0">
                    {viewMode === 'course' ? (
                        <LessonContent session={currentSession} />
                    ) : (
                        <AbapPlayground />
                    )}
                </div>
            </main>
        </div>
    );
}

export default App;
