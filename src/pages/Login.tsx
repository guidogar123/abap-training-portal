import React, { useState, useEffect } from 'react';
import { useNavigate, useLocation } from 'react-router-dom';
import { LayoutPanelTop, Monitor, GraduationCap, ShieldCheck } from 'lucide-react';
import { useAuth } from '../context/AuthContext';

export const Login: React.FC = () => {
    const { login, isAuthenticated, loading } = useAuth();
    const [msLoading, setMsLoading] = useState(false);
    const navigate = useNavigate();
    const location = useLocation();

    const from = location.state?.from?.pathname || '/';

    useEffect(() => {
        if (isAuthenticated) {
            navigate(from, { replace: true });
        }
    }, [isAuthenticated, navigate, from]);

    const handleLogin = async () => {
        setMsLoading(true);
        try {
            await login();
        } catch (error) {
            console.error(error);
            setMsLoading(false);
        }
    };

    return (
        <div className="min-h-screen bg-[#07090f] flex items-center justify-center p-4 relative overflow-hidden font-sans">
            {/* Ambient Background Effects */}
            <div className="absolute top-0 left-1/4 w-[500px] h-[500px] bg-blue-600/10 rounded-full blur-[120px] -translate-y-1/2 animate-pulse"></div>
            <div className="absolute bottom-0 right-1/4 w-[500px] h-[500px] bg-indigo-600/10 rounded-full blur-[120px] translate-y-1/2"></div>

            <div className="w-full max-w-xl relative z-10 transition-all duration-700 animate-in fade-in slide-in-from-bottom-8">
                <div className="text-center mb-10">
                    <div className="w-20 h-20 bg-gradient-to-br from-blue-600 to-indigo-600 rounded-2xl flex items-center justify-center mx-auto mb-6 shadow-2xl shadow-blue-500/20 rotate-3 transition-transform hover:rotate-0 duration-500">
                        <GraduationCap size={40} className="text-white" />
                    </div>
                    <h1 className="text-4xl font-extrabold text-white tracking-tight mb-3">
                        Portal de Capacitación ABAP
                    </h1>
                    <p className="text-slate-400 text-lg">
                        Plataforma exclusiva para consultores de <span className="text-blue-400 font-semibold">Sistemas e Información S.A.S</span>
                    </p>
                </div>

                <div className="bg-slate-900/40 backdrop-blur-2xl border border-white/10 rounded-3xl p-10 shadow-3xl overflow-hidden relative group">
                    <div className="absolute inset-0 bg-gradient-to-br from-blue-500/5 to-transparent opacity-0 group-hover:opacity-100 transition-opacity duration-700 pointer-events-none"></div>

                    <div className="grid grid-cols-1 gap-8 mb-10">
                        <div className="flex items-start gap-4 p-4 rounded-2xl bg-white/5 border border-white/5 transition-colors hover:bg-white/10">
                            <div className="p-2.5 bg-blue-500/10 rounded-xl text-blue-400">
                                <Monitor size={20} />
                            </div>
                            <div>
                                <h3 className="text-white font-semibold mb-1">Entorno de Práctica</h3>
                                <p className="text-slate-400 text-sm leading-relaxed">Accede a ejercicios reales y validador de código ABAP integrado.</p>
                            </div>
                        </div>

                        <div className="flex items-start gap-4 p-4 rounded-2xl bg-white/5 border border-white/5 transition-colors hover:bg-white/10">
                            <div className="p-2.5 bg-indigo-500/10 rounded-xl text-indigo-400">
                                <LayoutPanelTop size={20} />
                            </div>
                            <div>
                                <h3 className="text-white font-semibold mb-1">Rutas de Aprendizaje</h3>
                                <p className="text-slate-400 text-sm leading-relaxed">Temario estructurado de 4 semanas, desde ECC hasta SAP BTP/Cloud.</p>
                            </div>
                        </div>
                    </div>

                    <button
                        onClick={handleLogin}
                        disabled={msLoading || loading}
                        className="w-full bg-blue-600 hover:bg-blue-500 text-white font-bold py-4 rounded-2xl shadow-xl shadow-blue-600/20 flex items-center justify-center gap-3 transition-all active:scale-[0.98] disabled:opacity-50 group/btn h-16"
                    >
                        {msLoading ? (
                            <div className="w-6 h-6 border-3 border-white/30 border-t-white rounded-full animate-spin" />
                        ) : (
                            <>
                                <svg className="w-6 h-6 transition-transform group-hover/btn:scale-110" viewBox="0 0 23 23">
                                    <path fill="#f3f3f3" d="M0 0h11v11H0zM12 0h11v11H12zM0 12h11v11H0zM12 12h11v11H12z" />
                                </svg>
                                <span>Iniciar con Microsoft Outlook</span>
                            </>
                        )}
                    </button>

                    <div className="mt-8 flex items-center justify-center gap-2 text-slate-500 text-xs uppercase tracking-widest font-medium">
                        <ShieldCheck size={14} />
                        Acceso Seguro Corporativo
                    </div>
                </div>

                <p className="text-center text-slate-600 text-sm mt-8">
                    © 2026 Sistemas e Información S.A.S. Todos los derechos reservados.
                </p>
            </div>
        </div>
    );
};
